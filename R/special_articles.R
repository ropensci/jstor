flag_special_articles <- function(metadata) {
  metadata %>%
    mutate(special_article = dplyr::case_when(
      stringr::str_detect(.$article_title, "(E|e)rrat(a|um)") ~ "erratum",
      TRUE ~ "none"))
}
