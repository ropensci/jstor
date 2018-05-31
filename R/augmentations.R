#' Find total pages 
#' 
#' This function is a simple helper to calculate the total number of pages of
#' an article.
#' 
#' This function deals with three cases:
#' 
#' - if page_range is supplied, the number of pages is calculated from it.
#' - if only the first page is supplied, NA is returned
#' - if first and last page are supplied, the number of pages is calculated as
#' `last_page - first_page + 1`
#' 
#' @param first_page The first page of an article (numeric).
#' @param last_page The last page of an article (numeric).
#' @param page_range The page range of an article (character).
#' 
#' @return A vector with the calculated total pages.
#' 
#' @export
#' @examples 
#' # calculate pages from first and last page
#' first_pages <- sample(30:50, 10)
#' last_pages <- first_pages + sample(5:20, 10)
#' 
#' jst_get_total_pages(first_pages, last_pages)
#'
#' # get pages from page range
#' jst_get_total_pages(page_range = "51 - 70, 350")
jst_get_total_pages <- function(first_page, last_page, page_range) {
  ll <- list(first_page, last_page, page_range)
  if (!identical(length(unique(lengths(ll))), 1L)) {
    abort("All inputs must have the same length.")
  }
  
  case_when(
    is.na(first_page) & is.na(last_page) & is.na(page_range) ~ NA_real_,
    !is.na(page_range) ~ parse_ranges(page_range),
    is.na(page_range) & !is.na(first_page) & is.na(last_page) ~ NA_real_,
    is.na(page_range) & 
      !is.na(first_page) & 
        !is.na(last_page) ~ as.numeric(last_page) - as.numeric(first_page) + 1
  )
}




jst_add_total_pages <- function(meta_data, page_col = n_pages) {
  page_col <- rlang::enquo(page_col)
  dplyr::mutate(
    meta_data,
    !!page_col := jst_get_total_pages(first_page, last_page, page_range)
  )
}


jst_unify_journal_id <- function(meta_data) {
  meta_data %>%
    dplyr::mutate(journal_id = case_when(is.na(journal_pub_id) ~ journal_jcode,
                                         TRUE ~ journal_pub_id)) %>%
    dplyr::select(-journal_pub_id, -journal_jcode, -journal_doi)
}

jst_clean_page <- function(page) {
  # check if any has weird format like with AJS: AJSv104p126
  is_complicated <- str_detect(page, "p")
  
  # do not change missing values
  is_complicated[is.na(is_complicated)] <- FALSE
  
  page[is_complicated] <- stringr::str_extract(page[is_complicated], 
                                               "(?<=p)\\d+")
  
  convert_page(page)
}


jst_augment <- function(meta_data) {
  col_names <- names(meta_data)
  
  if (identical(col_names, names(article_cols$cols)) ||
      identical(col_names, names(article_cols_old$cols))) {
    # for journal articles
    meta_data %>%
      dplyr::mutate_at(vars("first_page", "last_page"), jst_clean_page) %>% 
      jst_unify_journal_id() %>%
      jst_add_total_pages()
    
  } else if (identical(col_names, names(book_cols$cols))) {
    # for books
    meta_data %>%
      dplyr::mutate_at(vars("first_page"), jst_clean_page) 
  } else {
    abort("Unknown meta_data type.")
  }
}



parse_ranges <- function(page_range) {
  splitted_df <- tibble::new_tibble(list(page_range = page_range)) %>% 
    mutate(id = 1:n(),
           splitted = stringr::str_split(page_range, ",|\\+")) %>% 
    tidyr::unnest()
  
  # detect roman numerals which are occasionally used for introduction sections
  roman_chars <- str_detect(splitted_df$splitted, "x|i|v|X|I|V")
  
  if (any(roman_chars, na.rm = T)) {
    warning("Cannot handle roman numerals (`x|i|v|X|I|V`) in rows (",
            paste(which(roman_chars), collapse = ", "),
            ") when computing ",
            "page range. ",
            "Returning `NA_character` instead.", call. = FALSE)
  }
  
  splitted_df %>% 
    mutate(wo_space = stringr::str_remove(splitted, "\\s"),
           wo_chars = stringr::str_remove_all(wo_space, "[:alpha:]"),
           first = stringr::str_extract(wo_chars, "^\\d+") %>% as.integer(),
           last = stringr::str_extract(wo_chars, "\\d+$") %>% as.integer(),
           total = last - first + 1) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise(sum = sum(total)) %>% 
    dplyr::pull(sum)
}

