#' Clean data from DfR
#' 
#' This function takes data from [jst_get_article()] and
#' applies helper functions for cleaning the data.
#' 
#' Data from DfR is inherently messy. For many examples see 
#' `vignette("known-quirks", package = "jstor")`. `jst_augment()` is a
#' convenience function that tries to deal with a few common tasks to
#' clean the data.
#' 
#' For journal articles, it calls [jst_clean_page()] to convert first and last
#' page, [jst_unify_journal_id()] and [jst_add_total_pages()].
#' 
#' @param meta_data Data which was processed via [jst_get_article()].
#' @param quietly Should warnings from converting page ranges be suppressed?
#' 
#' @return A cleaned tibble.
#' @seealso [jst_clean_page()] [jst_unify_journal_id()] [jst_add_total_pages()]
#' [jst_get_total_pages()]
#' 
#' @export
jst_augment <- function(meta_data, quietly = FALSE) {
  col_names <- names(meta_data)
  
  if (identical(col_names, names(article_cols$cols)) ||
      identical(col_names, names(article_cols_old$cols))) {
    # for journal articles
    meta_data %>%
      dplyr::mutate_at(dplyr::vars("first_page", "last_page"),
                       jst_clean_page) %>% 
      jst_unify_journal_id() %>%
      jst_add_total_pages(quietly = quietly)
  } else {
    abort("Unknown meta_data type.")
  }
}

#' Clean a character vector of pages
#' 
#' This function tries to convert character vectors into integers. This function
#' should not be called on page ranges.
#' 
#' @param page A character vector for pages.
#' @export
#' @examples 
#' jst_clean_page("2")
#' 
#' # anything that is not a digit gets removed
#' jst_clean_page("A2-")
#' 
#' # a weird format from the American Journal of Sociology is convered correctly
#' jst_clean_page("AJSv104p126")
#' # this is done by searching for "p", and if it is found, extracting the
#' # content after "p".
jst_clean_page <- function(page) {
  # check if any has weird format like with AJS: AJSv104p126
  is_complicated <- str_detect(page, "p")
  
  # do not change missing values
  is_complicated[is.na(is_complicated)] <- FALSE
  
  page[is_complicated] <- stringr::str_extract(page[is_complicated], 
                                               "(?<=p)\\d+")
  
  convert_page(page)
}

#' Add total count of pages
#' 
#' This function adds a column with the total count of pages. It calls 
#' [jst_get_total_pages()] which does the main work.
#' 
#' @param meta_data Data which was processed via [jst_get_article()].
#' @param quietly Should warnings from converting page ranges be suppressed?
#' @export
#' @seealso [jst_get_total_pages()]
jst_add_total_pages <- function(meta_data,
                                quietly = FALSE) {

  dplyr::mutate(
    meta_data,
    n_pages = jst_get_total_pages(first_page, last_page, page_range,
                                      quietly)
  )
}

#' Calculate total pages 
#' 
#' This function is a simple helper to calculate the total number of pages of
#' an article.
#' 
#' This function deals with four cases:
#' 
#' - if all three arguments are missing, NA is returned.
#' - if page_range is supplied, the number of pages is calculated from it.
#' - if only the first page is supplied, NA is returned.
#' - if first and last page are supplied, the number of pages is calculated as
#' `last_page - first_page + 1`.
#' 
#' The algorithm to parse page ranges works as follows: A typical page range is
#' `1-10, 200` where the article starts at page 1, ends at page 10, and has an
#' erratum at page 200. For this case, the range is calculated as
#' `range + single_page`, as in`(10 - 1 + 1) + 1 = 11`. Sometimes multiple 
#' ranges are given: `1-10, 11-20`. For those cases all ranges are summed:
#' `(10 - 1 + 1) + (20 - 11 + 1) = 20`. Another specification for multiple 
#' ranges is `1-10+11-20`, which is treated similarly.
#' 
#' 
#' @param first_page The first page of an article (numeric).
#' @param last_page The last page of an article (numeric).
#' @param page_range The page range of an article (character).
#' @param quietly Sometimes page ranges contain roman numerals like `xiv`. These
#' are not recognized, return `NA` and raise a warning. If set to `TRUE`, this
#' warning not raised.
#' 
#' @return A vector with the calculated total pages.
#' 
#' @export
#' @examples 
#' # calculate pages from first and last page
#' first_pages <- sample(30:50, 10)
#' last_pages <- first_pages + sample(5:20, 10)
#' page_ranges <- rep(NA_character_, 10)
#' 
#' jst_get_total_pages(first_pages, last_pages, page_ranges)
#'
#' # get pages from page range
#' jst_get_total_pages(NA_real_, NA_real_, "51 - 70")
#' jst_get_total_pages(NA_real_, NA_real_, "51 - 70, 350")
#' jst_get_total_pages(NA_real_, NA_real_, "350, 51 - 70")
#' jst_get_total_pages(NA_real_, NA_real_, "51 - 70, 80-100")
#' jst_get_total_pages(NA_real_, NA_real_, "51-70+350")
jst_get_total_pages <- function(first_page, last_page, page_range,
                                quietly = FALSE) {
  if (!(is.numeric(first_page) && is.numeric(last_page))) 
    abort("`first_page` and `last_page` must be numeric.")
  
  if (!is.character(page_range))
    abort("`page_range` must be a character vector.")
  
  ll <- list(first_page, last_page, page_range)
  if (!identical(length(unique(lengths(ll))), 1L)) {
    abort("All inputs must have the same length.")
  }
  
  dplyr::case_when(
    is.na(first_page) & is.na(last_page) & is.na(page_range) ~ NA_real_,
    !is.na(page_range) ~ parse_ranges(page_range, quietly),
    is.na(page_range) & !is.na(first_page) & is.na(last_page) ~ NA_real_,
    is.na(page_range) & 
      !is.na(first_page) & 
        !is.na(last_page) ~ as.numeric(last_page) - as.numeric(first_page) + 1
  )
}




#' Unify journal IDs
#' 
#' This function is a simple wrapper to unify journal ids.
#' 
#' Date on journal ids can be found in three columns:
#' `journal_pub_id`, `journal_jcode` and `journal_doi`. From my experience,
#' most of the time the relevant dat ais present in `journal_pub_id` or
#' `journal_jcode`, with `journal_jcode` being to most common identifier.
#' This function takes the value from `journal_pub_id`, and if it is missing,
#' that from `journal_jcode`. `journal_doi` is currently disregarded.
#' 
#' @param meta_data Data which was processed via [jst_get_article()].
#' @param remove_cols Should the original columns be removed after unifying?
#' 
#' @return A modified `tibble`.
#' 
#' @return A modified tibble.
#' @export
#' 
#' @examples 
#' article <- jst_get_article(jst_example("article_with_references.xml"))
#' 
#' jst_unify_journal_id(article)
#' 
#' 
#' # per default, original columns with data on the journal are removed
#' library(dplyr)
#' 
#' jst_unify_journal_id(article) %>% 
#'   select(contains("journal")) %>% 
#'   names()
#'   
#' # you can keep them by setting `remove_cols` to `FALSE`
#' jst_unify_journal_id(article, remove_cols = FALSE) %>%  
#'   select(contains("journal")) %>%
#'   names()
jst_unify_journal_id <- function(meta_data,
                                 remove_cols = TRUE) {
  out <- meta_data %>%
    dplyr::mutate(journal_id = dplyr::case_when(
      is.na(journal_pub_id) ~ journal_jcode,
      TRUE ~ journal_pub_id
    ))
  
  if (remove_cols) {
    out <- dplyr::select(out, -journal_pub_id, -journal_jcode, -journal_doi)
  } 
  
  out
}








parse_ranges <- function(page_range, quietly = FALSE) {
  nrow <- validate_tibble(page_range)
  
  splitted_df <- tibble::new_tibble(list(page_range = page_range), 
                                    nrow = nrow) %>% 
    mutate(id = 1:n(),
           splitted = stringr::str_split(page_range, ",|\\+")) %>% 
    tidyr::unnest(one_of("splitted"))
  
  # detect roman numerals which are occasionally used for introduction sections
  roman_chars <- str_detect(splitted_df$splitted, "x|i|v|X|I|V")
  
  if (any(roman_chars, na.rm = T) && !quietly) {
    warning("Cannot handle roman numerals (`x|i|v|X|I|V`) in rows (",
            paste(which(roman_chars), collapse = ", "),
            ") when computing ",
            "page range. ",
            "Returning `NA_character` instead.\n", call. = FALSE)
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

