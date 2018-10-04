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
#' combine `jst_get_references()` with `jst_get_footnotes()`.
#' 
#' For newer `xml`-files, there would be the option to extract single elements
#' like authors, title or date of the source, but this is not yet implemented.
#' 
#' In general, the implementation is not as fast as `jst_get_article()` -
#' articles with many references slow the process down.
#' 
#' @param file_path The path to the `.xml`-file from which references should be
#'   extracted.
#' @param parse_refs Should references be parsed, if available?
#'
#' @return A `tibble` with the following columns:
#'
#' - `file_name`: the identifier for the article the references come from.
#' - `ref_title`: the title of the references sections.
#' - `authors`: a string of authors. Several authors are separated with `;`.
#' - `collab`: a field that may contain information on the authors, if authors
#'             are not available.
#' - `title`: the title of the cited entry.
#' - `year`: a year, often the article's publication year, but not always. 
#' - `source`: the source of the cited entry. Might be a publisher or similar
#' - `unparsed_refs`: The full references entry in unparsed form.
#'
#' @export
#' @examples 
#' jst_get_references(jst_example("sample_with_references.xml"))
#' 
#' # import parsed references
#' jst_get_references(
#'   jst_example("parsed_references.xml"),
#'   parse_refs = TRUE
#' ) 
jst_get_references <- function(file_path, parse_refs = F) {
  xml_file <- read_jstor(file_path)

  validate_article(xml_file)

  # the file path is passed down to extract_ref_content to create more
  # informative error messages
  references <- extract_references(xml_file, file_path, parse_refs)

  expand_and_bind(file_path, references)
}


extract_references <- function(xml_file, file_path, parse_refs) {
  res <- xml_find_all(xml_file, ".//ref-list")

  # if there are no references, exit and return NA
  if (is_empty(res)) {
    return(new_tibble(list(
      ref_title = NA_character_,
      authors = NA_character_,
      collab = NA_character_,
      title = NA_character_,
      year = NA_character_,
      source = NA_character_,
      unparsed_refs = NA_character_
    )))
  }

  purrr::pmap(list(res, file_path, parse_refs), extract_ref_content) %>% 
    dplyr::bind_rows()

}


extract_ref_content <- function(x, file_path, parse_refs) {
  if (identical(xml2::xml_attr(x, "content-type"), "parsed-citations")) {
    if (parse_refs) {
      parse_references(x)
    } else {
      title <- extract_first(x, "title")
      x %>%
        xml_find_all("ref/mixed-citation") %>%
        map_chr(collapse_text) %>% 
        ref_to_tibble(title)
    }


  } else if (is.na(xml2::xml_attr(x, "content-type"))) {
    title <- extract_first(x, "title")
    x %>%
      xml_find_all("ref/mixed-citation/node()[not(self::*)]") %>%
      xml_text() %>%
      purrr::keep(str_detect, "[a-z]") %>%
      str_replace("^\\\n", "") %>% # remove "\n" at beginning of strings
      ref_to_tibble(title)
    
  } else if (identical(xml2::xml_attr(x, "content-type"), "unparsed") ||
             identical(xml2::xml_attr(x, "content-type"),
                       "unparsed-citations")) {
    
    title <- extract_first(x, "title")
    x %>%
      xml_find_all("ref/mixed-citation") %>%
      xml_text() %>% 
      ref_to_tibble(title)
    
  } else {
    abort(paste0("Unknown citation format in file `", file_path, "`.\n",
                 "Please file an issue at ",
                 "`https://github.com/ropensci/jstor/issues`."))
  }
}

collapse_text <- function(x) {
  xml_find_all(x, ".//text()") %>%
    xml_text() %>%
    paste(collapse = " ")
}

parse_references <- function(ref_list) {
  
  title <- extract_child(ref_list, "title")
  
  out <- ref_list %>% 
    xml_find_all("ref") %>% 
    map(~{
      list(
        authors = extract_all(., ".//string-name"),
        collab = extract_all(., ".//collab"),
        title = extract_first(., ".//article-title"),
        year = extract_all(., ".//year"),
        source = extract_first(., ".//source"),
        unparsed_refs = collapse_text(.)
      )
    }) %>% 
    dplyr::bind_rows()

  dplyr::bind_cols(ref_title = rep(title, nrow(out)), out)  
}


ref_to_tibble <- function(refs, title) {
  if (is_empty(refs)) {
    refs <- NA_character_
  }
  refs <- gsub("^$", NA_character_, refs)
  
  new_tibble(list(
    ref_title = rep(title, length(refs)),
    authors = rep(NA_character_, length(refs)),
    collab = rep(NA_character_, length(refs)),
    title = rep(NA_character_, length(refs)),
    year = rep(NA_character_, length(refs)),
    source = rep(NA_character_, length(refs)),
    unparsed_refs = refs
  ))
}
