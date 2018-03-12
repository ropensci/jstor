#' Extract meta information for articles
#'
#' `find_article()` extracts meta-data from JSTOR-XML files for journal
#' articles.
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

  xml_file <- read_jstor(file_path)

  validate_article(xml_file)

  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")

  # pages
  first_page <- extract_page(article, "fpage")
  last_page <- extract_page(article, "lpage")

  basename_id <- list(basename_id = get_basename(file_path))

  journal_ids <- extract_jcode(front)
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
    last_page = last_page
  )

  dplyr::bind_cols(basename_id, journal_ids, article_ids, out)
}


extract_jcode <- function(front) {
  journal_id <- front %>%
    xml_find_all("journal-meta/journal-id")

  # in the very improbable case, information on the journal is missing, exit
  # early
  if (is_empty(journal_id)) {
    return(list(journal_doi = NA_character_,
                journal_jcode = NA_character_,
                journal_pub_id = NA_character_))
  }


  doi <- extract_first(front, id_constructor("journal", "journal", "doi"))

  journal_pub_id <- extract_first(front, id_constructor("journal", "journal",
                                                        "publisher-id"))

  jcode <- extract_first(front, id_constructor("journal", "journal", "jstor"))

  list(journal_doi = doi, journal_jcode = jcode,
       journal_pub_id = journal_pub_id)
}

extract_article_id <- function(front) {
  article_id <- front %>%
    xml_find_all("article-meta/article-id")

  if (is_empty(article_id)) {
    return(list(article_doi = NA_character_,
                article_pub_id = NA_character_,
                article_jcode = NA_character_))
  }

  doi <- extract_first(front, id_constructor("article", "pub", "doi"))

  article_pub_id <- extract_first(front, id_constructor("article", "pub",
                                                        "publisher-id"))

  article_jcode <- extract_first(front, paste0("article-meta/article-id",
                                               "[@pub-id-type='jstor' or ",
                                               "@pub-id-type='jstor-stable']"))

  list(article_doi = doi, article_pub_id = article_pub_id,
       article_jcode = article_jcode)
}


id_constructor <- function(level_one, level_two, type) {
  paste0(level_one, "-meta/", level_one, "-id[@", level_two, "-id-type='", type,
         "']")
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
    !is.na(first_page) & is.na(last_page) ~ NA_real_,
    !is.na(first_page) & !is.na(last_page) ~ last_page - first_page + 1,
    TRUE ~ NA_real_
  )
}
