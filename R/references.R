#' Extract all references
#'
#' This function extracts the content of `ref-list` within the `xml`-file.
#'
#' @param file_path The path to the `.xml`-file from which references should be
#'   extracted.
#'
#' @return A `data.frame` with three columns containing the references:
#'
#' - `basename_id`: the identifier for the article the references come from.
#' - `full_reference`: the text of the references.
#' - `author_names`: in case the information is present in the `xml`-file, names
#' of the authors for each article. 
#'
#' Due to this structure, a single reference may span multiple rows. Each row
#' then has only one author in `author_names`. Mind the fact, that data quality
#' from JSTOR is mixed. For many articles there will be no data in
#' `author_names`.
#' For some there will be data, but only incomplete, i.e. if the article has 3
#' authors, there might be only the first author in `author_names`.

#' @export
#' @examples 
#' find_references(jstor_example("sample_with_references.xml"))
find_references <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  validate_article(xml_file)
  
  references <- extract_references(xml_file) %>% 
    rlang::set_names("references") %>% 
    new_tibble()
  
  expand_and_bind(file_path, references)
}


extract_references <- function(xml_file) {
  res <- xml_find_all(xml_file, ".//ref-list")

  # if there are no references, exit and return NA
  if (is_empty(res)) {
    return(list(NA_character_))
  }

  full_string <- res %>% 
    map(extract_ref_content) %>% 
    flatten_chr()

  # empty strings should be NA
  if (is_empty(full_string)) {full_string <- NA_character_}
  full_string <- gsub("^$", NA_character_, full_string)

  list(full_string)
}


extract_ref_content <- function(x) {
  if (identical(xml2::xml_attr(x, "content-type"), "parsed-citations")) {
    message("Parsed citations are not fully supported yet.")
    x %>%
      xml_find_all("title|ref/mixed-citation") %>% 
      map_chr(collapse_text)
    
  } else if (is.na(xml2::xml_attr(x, "content-type"))) {
    x %>%
      xml_find_all("title|ref/mixed-citation/node()[not(self::*)]") %>%
      xml_text() %>% 
      purrr::keep(str_detect, "[a-z]") %>%
      str_replace("^\\\n", "") # remove "\n" at beginning of strings
    
  } else if (identical(xml2::xml_attr(x, "content-type"), "unparsed")) {
    x %>%
      xml_find_all("title|ref/mixed-citation") %>%
      xml_text()
  }
}

collapse_text <- function(x) {
  xml_find_all(x, ".//text()") %>%
    xml_text() %>%
    paste(collapse = " ")
}
