# nocov start

#' Defunct: Extract meta information for articles
#'
#' This function will be removed in the next release. Please use
#' `jst_get_article()` instead.
#'
#' @param file_path A `.xml`-file for a journal-article.
#'
#' @return A `tibble` containing the extracted meta-data with the following
#' columns:
#' - basename_id *(chr)*: The filename of the original .xml-file. Can be used 
#'   for joining with other parts (authors, references, footnotes, full-texts).
#' - journal_doi *(chr)*: A registered identifier for the journal.
#' - journal_jcode *(chr)*: A identifier for the journal like "amerjsoci" for
#'   the "American Journal of Sociology".
#' - journal_pub_id *(chr)*: Similar to journal_jcode. Most of the time either
#'   one is present.
#' - journal_title *(chr)*: The title of the journal.
#' - article_doi *(chr)*: A registered unique identifier for the article.
#' - article_jcode *(chr)*: A unique identifier for the article (not a DOI).
#' - article_pub_id *(chr)*: Infrequent, either part of the DOI or the 
#'   article_jcode.
#' - article_type *(chr)*: The type of article (research-article, book-review,
#'   etc.).
#' - article_title *(chr)*: The title of the article.
#' - volume *(chr)*: The volume the article was published in.
#' - issue *(chr)*: The issue the article was published in.
#' - language *(chr)*: The language of the article.
#' - pub_day *(chr)*: Publication day, if specified.
#' - pub_month *(chr)*: Publication month, if specified.
#' - pub_year *(int)*: Year of publication.
#' - first_page *(int)*: Page number for the first page of the article.
#' - last_page *(int)*: Page number for the last page of the article.
#' - page_range *(chr)*: The range of pages for the article.
#'
#' A note about publication dates: always the first entry is being extracted,
#' which should correspond to the oldest date, in case there is more than one
#' date.
#'
#' @export
#' @examples
#' \dontrun{ 
#' find_article(jstor_example("sample_with_references.xml"))
#' }
find_article <- function(file_path) {
  base::.Defunct(msg = paste("`find_article` has been deprecated.",
                                "Please use `jst_get_article()` instead."))
  xml_file <- read_jstor(file_path)
  
  validate_article(xml_file)
  
  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")
  
  # pages
  first_page <- extract_page(article, "fpage", convert = FALSE)
  last_page <- extract_page(article, "lpage", convert = FALSE)
  
  basename_id <- list(basename_id = get_basename(file_path))
  
  journal_ids <- extract_jcode(front)
  journal_title <- list(
    journal_title = extract_child(front, ".//journal-title")
  )
  
  article_ids <- extract_article_id(front)
  
  out <- list(
    article_type = xml2::xml_attr(xml_file, "article-type"),
    article_title = extract_title(article),
    volume = extract_child(article, "volume"),
    issue = extract_child(article, "issue"),
    language = extract_child(article, ".//meta-value"),
    # the XPATH for the dates grabs always the first date by default.
    # dates like "May" get turned into NA
    pub_day = extract_child(article, ".//day"),
    pub_month = extract_child(article, ".//month"),
    pub_year = extract_child(article, ".//year") %>% as.integer(),
    first_page = first_page,
    last_page = last_page,
    page_range = extract_child(article, "page-range")
  )
  
  dplyr::bind_cols(basename_id, journal_ids, journal_title, article_ids, out)
}

#' Defunct: Extract author information
#'
#' This function will be removed in the next release. Please use
#' `jst_get_authors()` instead.
#'
#' The function returns a `tibble` with the following six columns:
#' - *prefix*: in case there was a prefix to the name, like `"Dr."`.
#' - *given_name*: The author's given name, like `"Albert"`.
#' - *surname*: The author's surname like `"Einstein"`.
#' - *string_name*: In some cases data the name is not available in separate
#'   fields, but just as a complete string: `"Albert Einstein"`.
#' - *suffix*: a suffix to the name, like `"Jr."`.
#' - *author_number*: The authors are enumerated in the order they appear in the
#'   data.
#'
#' @param file_path A `.xml`-file from JSTOR containing meta-data.
#'
#' @return A `tibble` containing the extracted authors. All empty fields are
#' `NA_character`.
#'
#' @export
#' @examples
#' \dontrun{ 
#' find_authors(jstor_example("sample_with_references.xml"))
#' }
find_authors <- function(file_path) {
  base::.Defunct(msg = paste("`find_authors` has been deprecated.",
                                "Please use `jst_get_authors()` instead."))
  xml_file <- read_jstor(file_path)
  
  if (identical(xml2::xml_name(xml_file), "article")) {
    front <- xml_find_all(xml_file, "front")
    meta <- xml_child(front, "article-meta")
  } else if (identical(xml2::xml_name(xml_file), "book")) {
    meta <- xml_find_all(xml_file, "book-meta")
  }
  
  authors <- extract_authors(meta)
  
  expand_and_bind(file_path, authors)
}


#' Defunct: Wrapper for file import
#'
#' This function will be removed in the next release. Please use
#' `jst_import()` instead.
#' 
#' This function applies an import function to a list of `xml`-files
#' or a .zip-archive in case of `jstor_import_zip` and saves
#' the output in batches of `.csv`-files to disk.
#'
#' Along the way, we wrap three functions, which make the process of converting 
#' many files easier:
#' 
#' - [purrr::safely()]
#' - `foreach::foreach()`
#' - [readr::write_csv()]
#' 
#' When using one of the `find_*` functions, there should usually be no errors.
#' To avoid the whole computation to fail in the unlikely event that an error
#' occurs, we use `safely()` which let's us
#' continue the process, and catch the error along the way.
#' 
#' If you have many files to import, you might benefit from executing the
#' function in parallel. We use
#' \code{snow::createCluster()} to setup a cluster
#' and
#' then compute the results via `foreach` and \code{\%dopar\%}. The type of
#' cluster is determined by `getClusterOption("type")`.
#' 
#' After importing all files, they are written to disk with
#' [readr::write_csv()].
#' 
#' Since you might run out of memory when importing a large quantity of files,
#' the files to import are split up into batches. Each batch is being treated
#' separately, therefore for each batch multiple processes from
#' \code{snow::createCluster()} are
#' spawned. For this reason, it is not recommended to have very small batches,
#' as there is an overhead for starting and ending the processes. On the other
#' hand, the batches should not be too large, to not exceed memory limitations.
#' A value of 10000 to 20000 for `files_per_batch` should work fine on most
#' machines. If the session is interactive and `show_progress` is `TRUE`, a
#' progress bar is displayed for each batch.
#' 
#' 
#' @param in_paths A character vector to the `xml`-files which should be 
#' imported
#' @param out_file Name of files to export to. Each batch gets appended by an
#' increasing number.
#' @param out_path Path to export files to (combined with filename).
#' @param .f Function to use for import. Can be one of `find_article`,
#' `find_authors`, `find_references`, `find_footnotes`, `find_book` or
#' `find_chapter`.
#' @param col_names Should column names be written to file? Defaults to `TRUE`.
#' @param n_batches Number of batches, defaults to 1.
#' @param files_per_batch Number of files for each batch. Can be used instead of
#' n_batches, but not in conjunction.
#' @param cores Number of cores to use for parallel processing.
#' @param show_progress Displays a progress bar for each batch, if the session
#' is interactive.
#'
#' @return Writes `.csv`-files to disk.
#'
#' @seealso [jst_combine_outputs()]
#' @export
#' @examples
#' \dontrun{
#' # read from file list --------
#' # find all files
#' meta_files <- list.files(pattern = "xml", full.names = T)
#' 
#' # import them via `find_article`
#' jstor_import(meta_files, out_file = "imported_metadata", .f = find_article,
#'              files_per_batch = 25000, cores = 4)
#'
#' # read from zip archive ------ 
#' # define imports
#' imports <- jst_define_import(article = c(find_article, find_authors))
#' 
#' # convert the files to .csv
#' jstor_import_zip("my_archive.zip", out_file = "my_out_file", 
#'                  import_spec = imports)
#' } 
jstor_import <- function(in_paths, out_file, out_path = NULL, .f,
                         col_names = TRUE, n_batches = NULL,
                         files_per_batch = NULL,
                         cores = getOption("mc.cores", 1L),
                         show_progress = TRUE) {
  base::.Defunct(msg = paste("`jstor_import` has been deprecated.",
                                "Please use `jst_import()` instead."))
  
  if (!is.null(n_batches) && !is.null(files_per_batch)) {
    stop("Either n_batches or files_per_batch needs to be specified, ",
         "not both.", call. = FALSE)
  }
  start_time <- Sys.time()
  
  message("Starting to import ", format(length(in_paths), big.mark = ","),
          " file(s).")
  
  # set n_batches to 1 for default
  if (is.null(n_batches) && is.null(files_per_batch)) {
    n_batches <- 1
  }
  
  if (!is.null(files_per_batch)) {
    file_list <- split(in_paths, ceiling(seq_along(in_paths) / files_per_batch))
  } else if (identical(as.integer(n_batches), 1L)) {
    file_list <- list(`1` = in_paths)
  } else {
    file_list <- split(in_paths, as.integer(cut(seq_along(in_paths), n_batches)))
  }
  
  chunk_numbers <- unique(names(file_list)) %>% as.list()
  
  if (!is.null(out_path)) {
    out_file <- file.path(out_path, out_file)
  }
  
  n_batches <- length(chunk_numbers)
  
  purrr::pwalk(
    list(file_list, chunk_numbers, out_file, list(.f), cores = cores,
         col_names = col_names, n_batches = n_batches,
         show_progress = show_progress),
    jstor_convert_to_file
  )
  
  end_time <- Sys.time()
  run_time <- end_time - start_time
  message("Finished importing ", format(length(in_paths), big.mark = ","),
          " file(s) in ", format(round(run_time, 2)), ".")
}

#' Defunct: Extract meta information for books
#'
#' This function will be removed in the next release. Please use
#' `jst_get_book()` instead.
#'
#' `find_book()` extracts meta-data from JSTOR-XML files for book chapters.
#'
#' @param file_path A `.xml`-file for a book or research report.
#' 
#' @return A `tibble` containing the extracted meta-data with the following
#' columns:
#' - basename_id *(chr)*: The filename of the original .xml-file. Can be used 
#'   for joining with other data for the same file.
#' - discipline *(chr)*: The discipline from the discipline names used on JSTOR.
#' - book_id *(chr)*: The book id of type "jstor", which is not a registered 
#'   DOI.
#' - book_title *(chr)*: The title of the book.
#' - book_subtitle *(chr)*: The subtitle of the book.
#' - pub_day *(int)*: Publication day, if specified.
#' - pub_month *(int)*: Publication month, if specified.
#' - pub_year *(int)*: Year of publication.
#' - isbn *(chr)*: One or more entries for the book's ISBN. If two or more, 
#'   separated by `"; "`.
#' - publisher_name *(chr)*: The name of the publisher.
#' - publisher_loc *(chr)*: The location of the publisher.
#' - n_pages *(int)*: The number of pages.
#' - language *(chr)*: The language of the book.
#'
#' A note about publication dates: always the first entry is being extracted,
#' which should correspond to the oldest date, in case there is more than one
#' date.
#' 
#' @export
#' @examples
#' \dontrun{ 
#' find_book(jstor_example("sample_book.xml"))
#' }
find_book <- function(file_path) {
  base::.Defunct(msg = paste("`find_book` has been deprecated.",
                                "Please use `jst_get_book()` instead."))
  xml_file <- read_jstor(file_path)
  
  validate_book(xml_file)
  
  book <- xml_find_all(xml_file, "book-meta")
  
  out <- list(
    book_id = extract_child(book, ".//book-id"),
    basename_id = get_basename(file_path),
    discipline = extract_all(
      book, ".//subj-group[@subj-group-type='discipline']"
    ),
    book_title = extract_child(book, ".//book-title"),
    book_subtitle = extract_child(book, ".//book-title-group/subtitle"),
    pub_day = extract_child(book, ".//day") %>% as.integer(),
    pub_month = extract_child(book, ".//month") %>% as.integer(),
    pub_year = extract_child(book, ".//year") %>% as.integer(),
    isbn = extract_all(book, "isbn"),
    publisher_name = extract_child(book, ".//publisher-name"),
    publisher_location = extract_child(book, ".//publisher-loc"),
    n_pages = extract_book_pages(book),
    language = extract_child(book, ".//meta-value")
  )
  
  tibble::new_tibble(out)
}

#' Defunct: Extract information on book chapters
#' 
#' This function will be removed in the next release. Please use
#' `jst_get_chapters()` instead.
#' 
#' `find_chapters()` extracts meta-data from JSTOR-XML files for book chapters.
#' 
#' Currently, `find_chapters()` is quite a lot slower than most of the other 
#' functions. It is roughly 10 times slower than `find_book`, depending on the
#' number of chapters to extract.
#'
#' @param file_path The path to a `.xml`-file for a book or research report.
#' @param authors Extracting the authors is an expensive operation which makes
#' the function ~3 times slower, depending on the number of chapters and
#' the number of authors. Defaults to `FALSE`. Use `authors = TRUE` to
#' import the authors too.
#' 
#' @return A `tibble` containing the extracted meta-data with the following
#' columns:
#' - book_id *(chr)*: The book id of type "jstor", which is not a registered 
#'   DOI.
#' - basename_id *(chr)*: The filename of the original .xml-file. Can be used 
#'   for joining with other data for the same file.
#' - part_id *(chr)*: The id of the part.
#' - part_label *(chr)*: A label for the part, if specified.
#' - part_title *(chr)*: The title of the part.
#' - part_subtitle *(chr)*: The subtitle of the part, if specified.
#' - authors *(list)*: A list-column with information on the authors. Can be
#'   unnested with [tidyr::unnest()]. See the examples
#'   and [find_authors()].
#' - abstract *(chr)*: The abstract to the part.
#' - part_first_page *(chr)*: The page where the part begins.
#' 
#' @export
#' @examples
#' \dontrun{ 
#' # extract parts without authors
#' find_chapters(jstor_example("sample_book.xml"))
#' 
#' # import authors too
#' parts <- find_chapters(jstor_example("sample_book.xml"), authors = TRUE)
#' parts
#' 
#' tidyr::unnest(parts)
#' }
find_chapters <- function(file_path, authors = FALSE) {
  base::.Defunct(msg = paste("`find_chapters` has been deprecated.",
                                "Please use `jst_get_chapters()` instead."))
  xml_file <- read_jstor(file_path)
  
  validate_book(xml_file)
  
  parts <- xml_find_all(xml_file, "body") %>%
    xml_find_all("book-part/body/book-part/book-part-meta")
  
  # catch case with no parts
  if (purrr::is_empty(parts)) {
    parts_out <- new_tibble(list(
      part_id = NA_character_,
      part_label = NA_character_,
      part_title = NA_character_,
      part_subtitle = NA_character_,
      authors = NA_character_,
      abstract = NA_character_,
      part_first_page = NA_character_
    ))
  } else {
    parts_out <- purrr::map_df(parts, find_part, authors)
  }
  
  base <- list(
    book_id = extract_child(xml_file, ".//book-id") %>%
      rep(times = nrow(parts_out)),
    basename_id = get_basename(file_path) %>%
      rep(times = nrow(parts_out))
  )
  
  dplyr::bind_cols(base, parts_out)
}

#' Defunct: Get path to jstor example
#' 
#' This function will be removed in the next release. Please use
#' `jst_example()` instead.
#' 
#' jstor includes several sample files for demonstration purposes. This helper
#' makes them easy to access.
#' 
#' The code for this function was adapted from the package `readr`.
#' 
#' @param path Name of the example file. If `NULL`, the example files will be
#' listed.
#' @export
#' @examples
#' \dontrun{
#' jstor_example()
#' jstor_example("sample_with_references.xml") 
#' }
jstor_example <- function(path = NULL) {
  base::.Defunct(msg = paste("`jstor_example` has been deprecated.",
                                "Please use `jst_example()` instead."))
  if (is.null(path)) {
    dir(system.file("extdata", package = "jstor"))
  } else {
    system.file("extdata", path, package = "jstor", mustWork = TRUE)
  }
}

#' Defunct: Extract all footnotes
#'
#' This function will be removed in the next release. Please use
#' `jst_get_footnotes()` instead.
#' 
#' This function extracts the content of `fn-group` from journal-articles.
#' 
#' The `fn-group` usually contains footnotes corresponding to the article.
#' However, since footnotes are currently not fully supported by DfR, 
#' there is no comprehensive documentation on the different variants. `jstor`
#' therefore extracts the content of `fn-group` exactly as it appears in the
#' data. Because of this, there might be other content present than footnotes.
#' 
#' In order to get all available information on citation data, you might need to
#' combine `find_footnotes()` with `find_references()`.
#'
#' @param file_path The path to the `.xml`-file from which footnotes should be
#'   extracted.
#'
#' @return A `tibble` containing the content from `fn-group` (usually the
#' footnotes). If there were no footnotes, `NA_character` is returned for the
#' column `footnotes`.
#' 
#' @export
#' @examples
#' \dontrun{ 
#' find_footnotes(jstor_example("sample_with_footnotes.xml"))
#' }
find_footnotes <- function(file_path) {
  base::.Defunct(msg = paste("`find_footnotes` has been deprecated.",
                                "Please use `jst_get_footnotes()` instead."))
  xml_file <- read_jstor(file_path)
  
  validate_article(xml_file)
  
  footnotes <- extract_footnotes(xml_file)
  
  expand_and_bind(file_path, footnotes)
}

#' Defunct: Import full-text
#'
#' This function will be removed in the next release. Please use
#' `jst_get_full_text()` instead.
#' 
#' This function imports the full_text contents of a JSTOR-article.
#'
#' @param filename The path to the file.
#' @return A `tibble`, containing the file-path as id, the full content of
#' the file, and the encoding which was used to read it.
#'
#' @export
read_full_text <- function(filename) {
  base::.Defunct(msg = paste("`read_full_text` has been deprecated.",
                                "Please use `jst_get_full_text()` instead."))
  validate_file_path(filename, "txt")
  
  id <- get_basename(filename)
  
  encoding <- get_encoding(filename)
  
  text <- read_file(filename, locale = locale(encoding = encoding))
  
  new_tibble(
    list(
      basename_id = id, full_text = text, encoding = encoding
    )
  )
}

#' Defunct: Extract the basename of a path
#' 
#' This function will be removed in the next release. Please use
#' `jst_get_file_name()` instead.
#' 
#' This helper simply extracts the basename of a path and removes the extension,
#' e.g. `foo/bar.txt` is shortened to `bar`.
#' 
#' @param file_path A path to a file
#' @export
get_basename <- function(file_path) {
  base::.Defunct(msg = paste("`get_basename` has been deprecated.",
                                "Please use `jst_get_file_name()` instead."))
  if (inherits(file_path, "jstor_zip")) {
    file_path[["file_path"]] %>% 
      basename() %>% 
      tools::file_path_sans_ext()
  } else {
    basename(file_path) %>%
      tools::file_path_sans_ext()
  }
}

#' Defunct: Extract all references
#' 
#' This function will be removed in the next release. Please use
#' `jst_get_references()` instead.
#' 
#' This function extracts the content of `ref-list` from the `xml`-file.
#' 
#' This content may contain references or endnotes, depending on how the article
#' used citations. Since references are currently not fully supported by DfR, 
#' there is no comprehensive documentation on the different variants. `jstor`
#' therefore extracts the content of `ref-list` exactly as it appears in the
#' data. Because of this, there might be other content present than references.
#' 
#' In order to get all available information on citation data, you might need to
#' combine `find_references()` with `find_footnotes()`.
#' 
#' For newer `xml`-files, there would be the option to extract single elements
#' like authors, title or date of the source, but this is not yet implemented.
#' 
#' In general, the implementation is not as fast as `find_article()` - articles
#' with many references slow the process down.
#' 
#' @param file_path The path to the `.xml`-file from which references should be
#'   extracted.
#'
#' @return A `tibble` with three two containing the references:
#'
#' - `basename_id`: the identifier for the article the references come from.
#' - `references`: the text of the references.
#'
#' @export
#' @examples
#' \dontrun{ 
#' find_references(jstor_example("sample_with_references.xml"))
#' }
find_references <- function(file_path) {
  base::.Defunct(msg = paste("`find_references` has been deprecated.",
                                "Please use `jst_get_references()` instead."))
  xml_file <- read_jstor(file_path)
  
  validate_article(xml_file)
  
  # the file path is passed down to extract_ref_content to create more
  # informative error messages
  references <- extract_references(xml_file, file_path) %>%
    rlang::set_names("references") %>%
    new_tibble()
  
  expand_and_bind(file_path, references)
}

# nocov end
