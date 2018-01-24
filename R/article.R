#' Extract meta information for articles
#'
#' `find_article()` extracts meta-data from JSTOR-XML files for journal
#' articles.
#'
#' @param file_path A `.xml`-file for a journal-article.
#'
#' @return A `tibble` containing the extracted meta-data with the following
#' columns:
#' - journal_id *(chr)*: The jcode or a DOI. If both are present, the jcode
#'  (=publisher-id) is extracted.
#' - basename_id *(chr)*: The filename of the original .xml-file. Can be used 
#'   for joining with full-texts.
#' - article_id *(chr)*: The article id, either a combination of digits 
#'   (`pub-id-type="jstor"`) or a DOI (`pub-id-type="doi"`).
#' - article_title *(chr)*: The title of the article.
#' - volume *(chr)*: The volume the article was published in.
#' - issue *(chr)*: The issue the article was published in.
#' - language *(chr)*: The language of the article.
#' - pub_day *(int)*: Publication day, if specified.
#' - pub_month *(int)*: Publication month, if specified.
#' - pub_year *(int)*: Year of publication.
#' - first_pag *(int)*: Page number for the first page of the article.
#' - last_page *(int)*: Page number for the last page of the article.
#'
#' A note about publication dates: always the first entry is being extracted,
#' which should correspond to the oldest date, in case there is more than one
#' date.
#'
#' @export
#' @examples 
#' find_article(jstor_example("sample_with_references.xml"))
find_article <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)
  
  validate_article(xml_file)

  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")

  # pages
  first_page <- extract_page(article, "fpage")
  last_page <- extract_page(article, "lpage")

  journal_ids <- extract_jcode(front)

  out <- list(
    basename_id = extract_basename(file_path, type = "xml"),
    article_id = extract_child(article, "article-id"),
    article_type = xml2::xml_attr(xml_file, "article-type"),
    article_title = extract_title(article),
    volume = extract_child(article, "volume"),
    issue = extract_child(article, "issue"),
    language = extract_child(article, ".//meta-value"),
    # the XPATH for the dates grabs always the first date by default.
    # dates like "May" get turned into NA
    pub_day = extract_child(article, ".//day") %>% as.integer(),
    pub_month = extract_child(article, ".//month") %>% as.integer(),
    pub_year = extract_child(article, ".//year") %>% as.integer(),
    first_page = first_page,
    last_page = last_page
  )
  
  dplyr::bind_cols(journal_ids, out)
}


extract_jcode <- function(front) {
  journal_id <- front %>%
    xml_find_all("journal-meta/journal-id")
  
  # in the very improbable case, information on the journal is missing, exit
  # early
  if (is_empty(journal_id)) {
    return(list(journal_doi = NA_character_,
                journal_pub_id = NA_character_,
                jcode = NA_character_))
  }
  
  
  doi <- extract_first(front, id_constructor("journal", "journal", "doi"))
  
  journal_pub_id <- extract_first(front, id_constructor("journal", "journal",
                                                        "publisher-id"))
  
  jcode <- extract_first(front, id_constructor("journal", "journal", "jstor"))
  
  list(journal_doi = doi, journal_pub_id = journal_pub_id, jcode = jcode)
}


id_constructor <- function(level_one, level_two, type) {
  glue::glue("{level_one}-meta/{level_one}-id[@{level_two}-id-type='{type}']")
}

extract_title <- function(article) {
  # find title, but exclude possible references/footnotes
  title <- xml_find_all(article, ".//article-title/node()[not(self::xref)]")
  
  if (is_empty(title)) {
    return(NA_character_)
  } else {
    title %>% 
      xml_text() %>%
      paste(collapse = "") # in case there are more elements, collapse them
  }
}


# page helpers
extract_page <- function(article, element) {
  x <- extract_child(article, element)

  # check if there are any non-digits
  if (grepl("\\D", x)) {
    # if there are non-digits, replace them with a empty space
    gsub("\\D", "", x) %>%
      as.integer()
  } else {
    as.integer(x)
  }
}

find_total_pages <- function(first_page, last_page) {
  dplyr::case_when(
    !is.na(first_page) & is.na(last_page) ~ 1,
    !is.na(first_page) & !is.na(last_page) ~ last_page - first_page + 1,
    TRUE ~ NA_real_
  )
}
