#' Extract all references
#'
#' This function extracts the content of `ref-list` from the `xml`-file.
#' 
#' This content may contain references or endnotes, depending on how the article
#' used citations. Since references are currently not fully supported by DfR, 
#' there is no comprehensive documentation on the different variants. `jstor`
#' therefore extracts the content of `ref-list` exactly as it appears in the
#' data. Because of this, there might be other content present than references.
#' 
#' In order to get all available information on citation data, you might need to
#' combine `find_references()` with `find_footnotes()`.
#' 
#' For newer `xml`-files, there would be the option to extract single elements
#' like authors, title or date of the source, but this is not yet implemented.
#' 
#' In general, the implementation is not as fast as `find_article()` - articles
#' with many references slow the process down.
#' 
#' @param file_path The path to the `.xml`-file from which references should be
#'   extracted.
#'
#' @return A `tibble` with three two containing the references:
#'
#' - `basename_id`: the identifier for the article the references come from.
#' - `references`: the text of the references.
#'
#' @export
#' @examples 
#' find_references(jstor_example("sample_with_references.xml"))
find_references <- function(file_path) {
  xml_file <- read_jstor(file_path)

  validate_article(xml_file)

  # the file path is passed down to extract_ref_content to create more
  # informative error messages
  references <- extract_references(xml_file, file_path) %>%
    rlang::set_names("references") %>%
    new_tibble()

  expand_and_bind(file_path, references)
}


extract_references <- function(xml_file, file_path) {
  res <- xml_find_all(xml_file, ".//ref-list")

  # if there are no references, exit and return NA
  if (is_empty(res)) {
    return(list(NA_character_))
  }

  full_string <- purrr::pmap(list(res, file_path), extract_ref_content) %>% 
    flatten_chr()

  # empty strings should be NA
  if (is_empty(full_string)) {
    full_string <- NA_character_
  }
  full_string <- gsub("^$", NA_character_, full_string)

  list(full_string)
}


extract_ref_content <- function(x, file_path) {
  if (identical(xml2::xml_attr(x, "content-type"), "parsed-citations")) {
    x %>%
      xml_find_all("title|ref/mixed-citation") %>%
      map_chr(collapse_text)

  } else if (is.na(xml2::xml_attr(x, "content-type"))) {
    x %>%
      xml_find_all("title|ref/mixed-citation/node()[not(self::*)]") %>%
      xml_text() %>%
      purrr::keep(str_detect, "[a-z]") %>%
      str_replace("^\\\n", "") # remove "\n" at beginning of strings

  } else if (identical(xml2::xml_attr(x, "content-type"), "unparsed") ||
             identical(xml2::xml_attr(x, "content-type"),
                       "unparsed-citations")) {
    x %>%
      xml_find_all("title|ref/mixed-citation") %>%
      xml_text()
  } else {
    abort(paste0("Unknown citation format in file `", file_path, "`.\n",
                 "Please file an issue at ",
                 "`https://github.com/tklebel/jstor/issues`."))
    return(NA_character_)
  }
}

collapse_text <- function(x) {
  xml_find_all(x, ".//text()") %>%
    xml_text() %>%
    paste(collapse = " ")
}
