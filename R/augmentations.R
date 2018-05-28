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
jst_get_total_pages <- function(first_page = NULL, last_page = NULL,
                             page_range = NULL) {
  if (!is.null(page_range) && !is.na(page_range)) {
    stopifnot(is.character(page_range))
    
    first_page <- stringr::str_extract(page_range, "\\d+") %>% as.integer()
    second_page <- stringr::str_extract(page_range, "(?<=-\\s?)\\d+") %>% 
      as.integer()
    
    second_page - first_page + 1
    
  } else {
    stopifnot(is.numeric(first_page) && is.numeric(last_page) || 
                all(is.na(first_page)) || all(is.na(last_page)))
    
    dplyr::case_when(
      !is.na(first_page) & is.na(last_page) ~ NA_real_,
      !is.na(first_page) & !is.na(last_page) ~ last_page - first_page + 1,
      TRUE ~ NA_real_
    )
  }
}
