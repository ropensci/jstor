get_zip_content <- function(zip_archive) {
  files <- utils::unzip(zip_archive, list = TRUE)

  files %>% 
    tibble::as_tibble() %>% 
    mutate(type = stringr::str_extract(Name, "^.*?(?=\\/)"),
           meta_type = dplyr::case_when(
             type == "metadata" & str_detect(Name, "article") ~ "journal_article",
             type == "metadata" & str_detect(Name, "book") ~ "book_chapter",
             type == "metadata" & str_detect(Name, "report") ~ "research_report",
             type == "metadata" & str_detect(Name, "pamphlet") ~ "pamphlet",
             str_detect(type, "ngram") ~ type,
             TRUE ~ NA_character_)) %>% 
    dplyr::select(-Length, -Date)
}


#' Preview content of zip files
#' 
#' This function gives you a quick preview about what a .zip-file from DfR
#' contains.
#' 
#' @param zip_archive A path to a .zip-file from DfR
#' 
#' @return The function returns a tibble with three columns:
#' - *type*: metadata or some form of ngram
#' - *meta_type*: which type of metadata (book_chapter, journal article, ...)
#' - *n*: a count for each category
#' @export
#' @examples 
#' jst_preview_zip(jst_example("pseudo_dfr.zip"))
jst_preview_zip <- function(zip_archive) {
  get_zip_content(zip_archive) %>% 
    dplyr::group_by_("type") %>%
    dplyr::count_("meta_type") %>% 
    dplyr::ungroup()
}




specify_zip_loc <- function(zip_archive, file_path) {
  out <- structure(
    list(zip_archive = zip_archive,
         file_path = file_path),
    class = "jstor_zip"
  )
  
  out
}
