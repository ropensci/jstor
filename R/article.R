#' Extract meta information for articles
#'
#' `jst_get_article()` extracts meta-data from JSTOR-XML files for journal
#' articles.
#'
#' @param file_path A `.xml`-file for a journal-article.
#'
#' @return A `tibble` containing the extracted meta-data with the following
#' columns:
#' - file_name *(chr)*: The file_name of the original .xml-file. Can be used 
#'   for joining with other parts (authors, references, footnotes, full-texts).
#' - journal_doi *(chr)*: A registered identifier for the journal.
#' - journal_jcode *(chr)*: A identifier for the journal like "amerjsoci" for
#'   the "American Journal of Sociology".
#' - journal_pub_id *(chr)*: Similar to journal_jcode. Most of the time either
#'   one is present.
#' - journal_title *(chr)*: The title of the journal.
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
#' - page_range *(chr)*: The range of pages for the article.
#'
#' A note about publication dates: always the first entry is being extracted,
#' which should correspond to the oldest date, in case there is more than one
#' date.
#'
#' @export
#' @examples 
#' jst_get_article(jst_example("sample_with_references.xml"))
jst_get_article <- function(file_path) {
  xml_file <- read_jstor(file_path)
  
  validate_article(xml_file)
  
  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")
  
  # pages
  first_page <- extract_page(article, "fpage", convert = FALSE)
  last_page <- extract_page(article, "lpage", convert = FALSE)
  
  file_name <- list(file_name = jst_get_file_name(file_path))
  
  journal_ids <- extract_jcode(front)
  journal_title <- list(
    journal_title = extract_child(front, ".//journal-title")
  )
  
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
    last_page = last_page,
    page_range = extract_child(article, "page-range")
  )
  
  dplyr::bind_cols(file_name, journal_ids, journal_title, article_ids, out)
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
extract_page <- function(article, element, convert = TRUE) {
  x <- extract_child(article, element)

  if (convert) {
    convert_page(x)
  } else {
    # replace empty strings with missing
    gsub("^$", NA_character_, x)
  }
}

convert_page <- function(x) {
  # find entries with non-digits
  has_char <- grepl("\\D", x)
  # remove those entries
  x[has_char] <- gsub("\\D", "", x[has_char])
  # return integer
  as.integer(x)
}

