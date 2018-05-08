get_zip_content <- function(zip_archive) {
  files <- utils::unzip(zip_archive, list = TRUE)

  files %>% 
    tibble::as_tibble() %>% 
    mutate(type = str_extract(Name, "^.*?(?=\\/)"),
           meta_type = case_when(
             type == "metadata" & str_detect(Name, "article") ~ "journal_article",
             type == "metadata" & str_detect(Name, "book") ~ "book_chapter",
             type == "metadata" & str_detect(Name, "report") ~ "research_report",
             type == "metadata" & str_detect(Name, "pamphlet") ~ "pamphlet",
             str_detect(type, "ngram") ~ "ngram",
             TRUE ~ NA_character_)) %>% 
    select(-Length, -Date)
}




specify_zip_loc <- function(zip_archive, file_path) {
  out <- structure(
    list(zip_archive = zip_archive,
         file_path = file_path),
    class = "jstor_zip"
  )
  
  out
}
