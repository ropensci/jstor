#' Combine outputs from converted files
#' 
#' `jst_combine_outputs()` helps you to manage the multitude of files you might
#' receive after running [jst_import()] or [jst_import_zip()] with more than
#' one batch. 
#' 
#' Splitting the output of [jst_import()] or [jst_import_zip()] might be done
#' for multiple reasons, but in the end you possibly want to combine all outputs
#' into one file/data.frame. This function makes a few assumptions in order to
#' combine files: 
#' 
#' - Files with similar names (except for trailing dashes with numbers) belong
#'   together and will be combined into one file.
#' - The names of the combined files can be determined from the original files.
#'   If you want to combine `foo-1.csv` and `foo-2.csv`, the combined file will
#'   be `combined_foo.csv`.
#' - The directory only contains files which were imported via 
#'   [jst_import()] or [jst_import_zip()]. If the directory contains other
#'   `.csv` files, you should supply a character vector with paths to only those
#'   files, which you want to import.
#' 
#' @param path A path to a directory, containing .csv-files from
#'  [jst_import()] or [jst_import_zip()], or a vector of files which are to be
#'  imported.
#' @param write_to_file Should combined data be written to a file?
#' @param out_path A directory where to write the combined files. If no
#' directory is supplied and `write_to_file` is `TRUE`, the combined files are
#' written to `path`.
#' @param overwrite Should files be overwritten?
#' @param clean_up Do you want to remove the original batch files? Use with
#' caution.
#' @param warn Should warnings be raised, if the file type cannot be determined?
#' 
#' @return Either writes to disk, or returns a list with all combined files.
#' 
#' @examples
#' # set up a temporary directory
#' tmp <- tempdir()
#' 
#' # find multiple files
#' file_list <- rep(jst_example("article_with_references.xml"), 2)
#'
#' # convert and write to file
#' jst_import(file_list, "article", out_path = tmp, .f = jst_get_article,
#'              n_batches = 2, show_progress = FALSE)
#'              
#' # combine outputs
#' jst_combine_outputs(tmp)
#' list.files(tmp, "csv")
#' 
#' \dontrun{
#' # Trying to combine the files again raises an error.
#' jst_combine_outputs(tmp)
#' }
#' 
#' # this doesn't
#' jst_combine_outputs(tmp, overwrite = TRUE)
#' 
#' # we can remove the original files too
#' jst_combine_outputs(tmp, overwrite = TRUE, clean_up = TRUE)
#' list.files(tmp, "csv")
#' 
#' @seealso [jst_re_import()]
#' @export
jst_combine_outputs <- function(path, write_to_file = TRUE,
                                out_path = NULL, overwrite = FALSE, 
                                clean_up = FALSE, warn = TRUE) {
  
  if (length(path) < 2 && dir.exists(path)) {
    # if it is a directory, list all files
    path <- check_path(path)
    
    files <- list.files(path, pattern = "-\\d+.csv", full.names = T)
  } else {
    path %>% 
      purrr::walk(check_path)
    
    files <- path
    
    if (is.null(out_path)) {
      abort("You must specify `out_path` when importing files from a vector.")
    }
  }
  
  
  splitted_paths <- tibble::tibble(files = files) %>% 
    mutate(group = stringr::str_remove(files, "-\\d+\\.csv$")) %>% 
    split(.$group) %>% 
    purrr::map(~dplyr::pull(.data = ., files))
  
  if (is_empty(splitted_paths)) {
    stop("There are no files to combine in ", path, ".", call. = FALSE)
  }
  
  
  reader <- function(x) {
    message("Re-importing ", length(x), " batches.")
    purrr::map_df(x, jst_re_import, warn)
  }
  
  writer <- function(x, path) {
    message("Writing combined file `", path, "` to disk.")
    write_csv(x, path = path)
  }
  

  if (write_to_file) {
    # create out-path
    if (is.null(out_path)) {
      out_path <- file.path(path, paste0("combined_",
                                         basename(names(splitted_paths)),
                                         ".csv"))
    } else {
      out_path <- file.path(out_path, paste0("combined_",
                                             basename(names(splitted_paths)), 
                                             ".csv"))
    }
    
    if (any(file.exists(out_path)) && !overwrite) {
      abort(paste0("The file(s) `", paste0(out_path, collapse = "`, `"),
                   "` already exists. Do you want",
                   " `overwrite = TRUE`?"))
    }
    
    re_imported <- purrr::map(splitted_paths, reader)
    
    purrr::walk2(re_imported, out_path, writer)
  } else {
    return(purrr::map(splitted_paths, reader))
  }

  if (clean_up) {
    message("Deleting original batches.")
    purrr::walk(splitted_paths, file.remove)
  }
  
}

#' Re-import files 
#' 
#' `jst_re_import()` lets you re-import a file which was exported via 
#' [jst_import()] or [jst_import_zip()].
#' 
#' When attempting to re-import, a heuristic is applied. If the file has column
#' names which match the names from any of the `find_*` functions, the file
#' is read with the corresponding specifications. If no column names are
#' recognized, files are recognized based on the number of columns. Since both
#' references and footnotes have only two columns, the first line is inspected
#' for either `"Referenc...|Bilbio...|Endnote..."` or `"Footnote..."`.
#' In case there is still no match, the file is read with
#' [readr::read_csv()] with `guess_max = 5000` and a warning is raised.
#' 
#' @param file A path to a .csv file.
#' @param warn Should warnings be emitted, if the type of file cannot be
#' determined?
#' 
#' @seealso [jst_combine_outputs()]
#' @export
jst_re_import <- function(file, warn = TRUE) {
  file <- check_path(file)
  
  if (!identical(tools::file_ext(file), "csv")) {
    abort(paste("Only .csv-files which were generated by `jst_import` or",
                "`jst_import_zip` can be re-imported."))
  }
  
  sample_row <- readr::read_lines(file, n_max = 1) %>% 
    stringr::str_split(",") %>% 
    purrr::flatten_chr()
  
  # match by column names
  matches <- list(
    article = names(article_cols$cols),
    authors = names(author_cols$cols),
    book = names(book_cols$cols),
    chapter = names(chapter_cols$cols),
    chapter_w_authors = names(chapter_w_authors$cols),
    footnotes = names(footnote_cols$cols),
    references = names(reference_cols$cols),
    references_old = names(reference_cols_old$cols),
    ngram = names(ngram_cols$cols),
    error = names(error_cols$cols)
  ) %>% 
    purrr::map_lgl(identical, sample_row)
  
  
  if (any(matches)) {
    switch(names(which(matches)),
           article = read_csv(file, col_types = article_cols),
           authors = read_csv(file, col_types = author_cols),
           book = read_csv(file, col_types = book_cols),
           chapter = read_csv(file, col_types = chapter_cols),
           chapter_w_authors = read_csv(file, col_types = chapter_w_authors),
           footnotes = read_csv(file, col_types = footnote_cols),
           references = read_csv(file, col_types = reference_cols),
           references_old = read_csv(file, col_types = reference_cols_old),
           ngram = read_csv(file, col_types = ngram_cols),
           error = read_csv(file, col_types = error_cols))
  } else {
    # match by column length
    matches <- c(
      article = 19L,
      article_old = 17L,
      authors = 7L,
      book = 13L,
      chapter = 9L,
      chapter_w_authors = 15L,
      ngram = 3L,
      references  = 8L
    ) %>% 
      purrr::map_lgl(identical, length(sample_row))
    
    if (any(matches)) {
      switch(names(which(matches)),
             article = read_csv(file, col_types = article_cols,
                                col_names = names(article_cols$cols)),
             article_old = read_csv(file, col_types = article_cols_old,
                                    col_names = names(article_cols_old$cols)),
             authors = read_csv(file, col_types = author_cols,
                                col_names = names(author_cols$cols)),
             book = read_csv(file, col_types = book_cols,
                             col_names = names(book_cols$cols)),
             chapter = read_csv(file, col_types = chapter_cols,
                                col_names = names(chapter_cols$cols)),
             chapter_w_authors = read_csv(
               file, col_types = chapter_w_authors,
               col_names = names(chapter_w_authors$cols)
             ),
             ngram = read_csv(file, col_types = ngram_cols,
                              col_names = names(ngram_cols$cols)),
             references = read_csv(file, col_types = references_cols,
                                   col_names = names(reference_cols$cols)))
    } else {
      # try to guess which type our source file is.
      # only looking at the first row might lead to errors, but there is only so
      # much we can do to try guessing the type.
      if (any(str_detect(stringr::str_to_lower(sample_row),
                         "referen.*|biblio.*|endnote.*"))) {
        # we can be sure here, that we have the old format for references, since
        # the new format has 8 columns
        read_csv(file, col_types = reference_cols_old,
                 col_names = names(reference_cols_old$cols))
      } else if (any(str_detect(stringr::str_to_lower(sample_row),
                                         "footnote.*"))) {
        read_csv(file, col_types = footnote_cols,
                 col_names = names(footnote_cols$cols))
      } else {
        if (warn) {
          warning("Unable to distinguish type of source for file `", file, "`.\n",
                  "Reverting to `read_csv(x, guess_max = 5000)`.", 
                  call. = FALSE)
        }
        suppressMessages(read_csv(file, guess_max = 5000, col_names = F))
      }
    }
  }
}


article_cols <- cols(
  file_name = col_character(),
  journal_doi = col_character(),
  journal_jcode = col_character(),
  journal_pub_id = col_character(),
  journal_title = col_character(),
  article_doi = col_character(),
  article_pub_id = col_character(),
  article_jcode = col_character(),
  article_type = col_character(),
  article_title = col_character(),
  volume = col_character(),
  issue = col_character(),
  language = col_character(),
  pub_day = col_character(),
  pub_month = col_character(),
  pub_year = col_integer(),
  first_page = col_character(),
  last_page = col_character(),
  page_range = col_character()
)
article_cols_old <- cols(
  file_name = col_character(),
  journal_doi = col_character(),
  journal_jcode = col_character(),
  journal_pub_id = col_character(),
  article_doi = col_character(),
  article_pub_id = col_character(),
  article_jcode = col_character(),
  article_type = col_character(),
  article_title = col_character(),
  volume = col_character(),
  issue = col_character(),
  language = col_character(),
  pub_day = col_character(),
  pub_month = col_character(),
  pub_year = col_integer(),
  first_page = col_character(),
  last_page = col_character()
)

author_cols <- cols(
  file_name = col_character(),
  prefix = col_character(),
  given_name = col_character(),
  surname = col_character(),
  string_name = col_character(),
  suffix = col_character(),
  author_number = col_integer()
)

book_cols <- cols(
  book_id = col_character(),
  file_name = col_character(),
  discipline = col_character(),
  book_title = col_character(),
  book_subtitle = col_character(),
  pub_day = col_integer(),
  pub_month = col_integer(),
  pub_year = col_integer(),
  isbn = col_character(),
  publisher_name = col_character(),
  publisher_location = col_character(),
  n_pages = col_integer(),
  language = col_character()
)


chapter_cols <- cols(
  book_id = col_character(),
  file_name = col_character(),
  part_id = col_character(),
  part_label = col_character(),
  part_title = col_character(),
  part_subtitle = col_character(),
  authors = col_character(),
  abstract = col_character(),
  part_first_page = col_character()
)

chapter_w_authors <- cols(
  book_id = col_character(),
  file_name = col_character(),
  part_id = col_character(),
  part_label = col_character(),
  part_title = col_character(),
  part_subtitle = col_character(),
  abstract = col_character(),
  part_first_page = col_character(),
  file_name = col_character(),
  prefix = col_character(),
  given_name = col_character(),
  surname = col_character(),
  string_name = col_character(),
  suffix = col_character(),
  author_number = col_integer()
)

footnote_cols <- cols(
  file_name = col_character(),
  footnotes = col_character()
)
reference_cols_old <- cols(
  file_name = col_character(),
  references = col_character()
)
reference_cols <- cols(
  file_name = col_character(),
  ref_title = col_character(),
  authors = col_character(), 
  collab = col_character(),
  title = col_character(),
  year = col_character(),
  source = col_character(),
  unparsed_refs = col_character()
)

ngram_cols <- cols(
  file_name = col_character(),
  ngram = col_character(),
  n = col_integer()
)

error_cols <- cols(
  id = col_integer(),
  error_message = col_character()
)
