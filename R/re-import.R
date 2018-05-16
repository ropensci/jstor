jst_combine_outputs <- function(path, out_path = NULL, overwrite = FALSE,
                        clean_up = FALSE) {
  
  files <- list.files(path, pattern = "-\\d+.csv", full.names = T)
  
  splitted_paths <- tibble::tibble(files = files) %>% 
    mutate(group = stringr::str_remove(files, "-\\d+\\.csv$")) %>% 
    split(.$group) %>% 
    purrr::map(~dplyr::pull(.data = ., files))
  
  
  if (is.null(out_path)) {
    out_names <- file.path(path, paste0("combined_",
                                        basename(names(splitted_paths)),
                                        ".csv"))
  } else {
    out_names <- file.path(out_path, paste0("combined_",
                           basename(names(splitted_paths)), ".csv"))
  }

  
  if (any(file.exists(out_names)) && !overwrite) {
    abort(paste0("The file(s) `", paste0(out_names, collapse = "`, `"),
                 "`` already exists. Do you want",
                 " `overwrite = TRUE`?"))
  }
  
  
  helper_fun <- function(x, path) {
    message("Re-importing ", length(x), " batches.")
    re_imported <- purrr::map_df(x, jst_re_import)
    
    message("Writing combined file `", path, "` to disk.")
    write_csv(re_imported, path = path)
  }
  
  purrr::walk2(splitted_paths, out_names, helper_fun)

  if (clean_up) {
    message("Deleting original batches.")
    purrr::walk(splitted_paths, file.remove)
  }
  
}


jst_re_import <- function(file) {
  sample_row <- readr::read_lines(file, n_max = 1) %>% 
    stringr::str_split(",") %>% 
    purrr::flatten_chr()
  
  # match by name
  matches <- list(
    article = names(article_cols$cols),
    authors = names(author_cols$cols),
    book = names(book_cols$cols),
    chapter = names(chapter_cols$cols),
    chapter_w_authors = names(chapter_w_authors$cols),
    footnotes = names(footnote_cols$cols),
    references = names(reference_cols$cols),
    ngram = names(ngram_cols$cols)
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
           ngram = read_csv(file, col_types = ngram_cols))
  } else {
    # match by column length
    matches <- c(
      article = 19L,
      article_old = 17L,
      authors = 6L,
      book = 13L,
      chapter = 9L,
      chapter_w_authors = 15L,
      ngram = 3L
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
                              col_names = names(ngram_cols$cols)))
    } else {
      # try to guess which type our source file is.
      # only looking at the first row might lead to errors, but there is only so
      # much we can do to try guessing the type.
      if (any(str_detect(sample_row, "Referen.*|Biblio.*"))) {
        read_csv(file, col_types = reference_cols,
                 col_names = names(reference_cols$cols))
      } else if (any(str_detect(sample_row, "Footnote.*|Endnote.*"))) {
        read_csv(file, col_types = footnote_cols,
                 col_names = names(footnote_cols$cols))
      } else {
        warning("Unable to distinguish type of source for file `", file, "`.\n",
                "Reverting to `read_csv(x, guess_max = 5000)`.", 
                call. = FALSE)
        suppressMessages(read_csv(file, guess_max = 5000))
      }
    }
  }
}


article_cols <- cols(
  basename_id = col_character(),
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
  pub_year = col_character(),
  first_page = col_character(),
  last_page = col_character(),
  page_range = col_character()
)
article_cols_old <- cols(
  basename_id = col_character(),
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
  pub_year = col_character(),
  first_page = col_character(),
  last_page = col_character()
)

author_cols <- cols(
  basename_id = col_character(),
  prefix = col_character(),
  given_name = col_character(),
  surname = col_character(),
  string_name = col_character(),
  suffix = col_character(),
  author_number = col_integer()
)

book_cols <- cols(
  book_id = col_character(),
  basename_id = col_character(),
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
  basename_id = col_character(),
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
  basename_id = col_character(),
  part_id = col_character(),
  part_label = col_character(),
  part_title = col_character(),
  part_subtitle = col_character(),
  abstract = col_character(),
  part_first_page = col_character(),
  basename_id = col_character(),
  prefix = col_character(),
  given_name = col_character(),
  surname = col_character(),
  string_name = col_character(),
  suffix = col_character(),
  author_number = col_integer()
)

footnote_cols <- cols(
  basename_id = col_character(),
  footnotes = col_character()
)
reference_cols <- cols(
  basename_id = col_character(),
  references = col_character()
)

ngram_cols <- cols(
  basename_id = col_character(),
  ngram = col_character(),
  n = col_integer()
)
