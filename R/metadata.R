#' Extract meta information
#'
#' `find_metadata()` extracts meta-data from JSTOR-XML files.
#'
#' **Publication dates**:
#'
#' Always the first entry is being extracted, which should correspond to the oldest
#' date, in case there is more than one date.
#'
#' @param file_path A file from JSTOR containing meta-data which can be read in with
#' [xml2::read_xml()]. `find_metadata` takes care of importing the file.
#'
#' @return A `tibble` containing the extracted meta-data.
#'
#' @export
#' @examples 
#' find_metadata(jstor_example("sample_with_references.xml"))
find_metadata <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)
  
  validate_doc_type(xml_file, "article", "book")

  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")

  # pages
  first_page <- extract_page(article, "fpage")
  last_page <- extract_page(article, "lpage")


  out <- data.frame(
    journal_id = extract_jcode(front),
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
    last_page = last_page,
    stringsAsFactors = FALSE
  )
  
  structure(out, class = c("jstor", "data.frame"))
}


extract_jcode <- function(front) {
  # Algorithm: if both files have journal-id-type = 'jstor', then we take the
  # first one, since this is always the short name.
  #
  # In case there is a publisher-id, take this one.

  journal_id <- front %>%
    xml_find_all("journal-meta/journal-id")
  
  # in the very improbable case, information on the journal is missing, exit
  # early
  if (is_empty(journal_id)) {
    return(NA_character_)
  }

  journal_id_attributes <- journal_id %>%
    xml2::xml_attr("journal-id-type") %>%
    unique()

  if (identical(journal_id_attributes, "jstor")) {
    xml_text(journal_id)[1]
  } else if ("publisher-id" %in% journal_id_attributes) {
    front %>%
      xml_find_first("journal-meta/journal-id[@journal-id-type='publisher-id']") %>%
      xml_text()
  }
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
  dplyr::case_when(!is.na(first_page) & is.na(last_page) ~ 1,
                   !is.na(first_page) & !is.na(last_page) ~ last_page - first_page + 1,
                   TRUE ~ NA_real_
  )
}
