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
#' - `ref_authors`: a string of authors. Several authors are separated with `;`.
#' - `ref_editors`: a string of editors, if available.
#' - `ref_collab`: a field that may contain information on the authors, if authors
#'             are not available.
#' - `ref_item_title`: the title of the cited entry. For books this is often 
#'    empty, with the title being in `ref_source`.
#' - `ref_year`: a year, often the article's publication year, but not always. 
#' - `ref_source`: the source of the cited entry. For books often the title of the
#'    book, for articles the publisher of the journal.
#' - `ref_volume`: the volume of the journal article.
#' - `ref_first_page`: the first page of the article/chapter.
#' - `ref_last_page`: the last page of the article/chapter.
#' - `ref_publisher`: For books the publisher, for articles often missing.
#' - `ref_publication_type`: Known types: `book`, `journal`, `web`, `other`.
#' - `ref_unparsed`: The full references entry in unparsed form.
#'
#' @export
#' @examples 
#' jst_get_references(jst_example("article_with_references.xml"))
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
      ref_authors = NA_character_,
      ref_editors = NA_character_,
      ref_collab = NA_character_,
      ref_item_title = NA_character_,
      ref_year = NA_character_,
      ref_source = NA_character_,
      ref_volume = NA_character_,
      ref_first_page = NA_character_,
      ref_last_page = NA_character_,
      ref_publisher = NA_character_,
      ref_publication_type = NA_character_,
      ref_unparsed = NA_character_
    ), nrow = 1L))
  }

  purrr::pmap(list(res, file_path, parse_refs), extract_ref_content) %>% 
    dplyr::bind_rows()

}


extract_ref_content <- function(x, file_path, parse_refs) {
  if (identical(xml2::xml_attr(x, "content-type"), "parsed-citations") ||
      identical(xml2::xml_attr(x, "content-type"), "references")) {
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
    map(parse_ref_content) %>% 
    dplyr::bind_rows()

  dplyr::bind_cols(ref_title = rep(title, nrow(out)), out)  
}


parse_ref_content <- function(ref) {
  
  # detect whether we have surnames
  surname <- xml2::xml_find_first(ref, ".//surname") 

  if (is.na(surname)) {
    authors <- extract_all(ref, ".//person-group[@person-group-type='author']")
    editors <- extract_all(ref, ".//person-group[@person-group-type='editor']")
    
    # if we have no precise data about authors or editors, we simply extract all
    # possible authors.
    if (is.na(authors)) {
      authors <- extract_all(ref, ".//string-name")
    }
  } else {
    authors <- collapse_names(ref, ".//person-group[@person-group-type='author']")
    editors <- collapse_names(ref, ".//person-group[@person-group-type='editor']")
  }
 
  list(
    ref_authors = authors,
    ref_editors = editors,
    ref_collab = extract_all(ref, ".//collab"),
    ref_item_title = extract_first(ref, ".//article-title"),
    ref_year = extract_all(ref, ".//year"),
    ref_source = extract_first(ref, ".//source"),
    ref_volume = extract_first(ref, ".//volume"),
    ref_first_page = extract_first(ref, ".//fpage"),
    ref_last_page = extract_first(ref, ".//lpage"),
    ref_publisher = extract_first(ref, ".//publisher-name"),
    ref_publication_type = extract_child(ref, "mixed-citation/@publication-type"),
    ref_unparsed = collapse_text(ref)
  )
  
  
}

collapse_names <- function(ref, xpath) {
  # find the base (e.g. person groups)
  base <- xml_find_all(ref, xpath)
  
  surnames <- xml_find_all(base, ".//surname") %>% xml_text()
  given_names <- xml_find_all(base, ".//given-names") %>% xml_text()
  
  res <- paste(surnames, given_names, sep = ", ") %>% 
    paste(collapse = "; ")
  
  if (res == "") {
    res <- NA_character_
  }
  
  res
}


ref_to_tibble <- function(refs, title) {
  if (is_empty(refs)) {
    refs <- NA_character_
  }
  refs <- gsub("^$", NA_character_, refs)
  
  out <- list(
    ref_title = rep(title, length(refs)),
    ref_authors = rep(NA_character_, length(refs)),
    ref_collab = rep(NA_character_, length(refs)),
    ref_item_title = rep(NA_character_, length(refs)),
    ref_year = rep(NA_character_, length(refs)),
    ref_source = rep(NA_character_, length(refs)),
    ref_volume = rep(NA_character_, length(refs)),
    ref_first_page = rep(NA_character_, length(refs)),
    ref_last_page = rep(NA_character_, length(refs)),
    ref_publisher = rep(NA_character_, length(refs)),
    ref_publication_type = rep(NA_character_, length(refs)),
    ref_unparsed = refs
  )
  
  nrow <- validate_tibble(out)
  
  new_tibble(out, nrow = nrow)
}
