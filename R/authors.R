#' Extract author information
#'
#' `find_authors()` extracts information about authors from JSTOR-XML files.
#'
#' The function returns a `tibble` with the following six columns:
#' - *prefix*: in case there was a prefix to the name, like `"Dr."`.
#' - *given_name*: The author's given name, like `"Albert"`.
#' - *surname*: The author's surname like `"Einstein"`.
#' - *string_name*: In some cases data the name is not available in separate
#'   fields, but just as a complete string: `"Albert Einstein"`.
#' - *suffix*: a suffix to the name, like `"Jr."`.
#' - *author_number*: The authors are enumerated in the order they appear in the
#'   data.
#'
#' @param file_path A `.xml`-file from JSTOR containing meta-data.
#'
#' @return A `tibble` containing the extracted authors. All empty fields are
#' `NA_character`.
#'
#' @export
#' @examples 
#' find_authors(jstor_example("sample_with_references.xml"))
find_authors <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  if (identical(xml2::xml_name(xml_file), "article")) {
    front <- xml_find_all(xml_file, "front")
    meta <- xml_child(front, "article-meta")
  } else if (identical(xml2::xml_name(xml_file), "book")) {
    meta <- xml_find_all(xml_file, "book-meta")
  }

  authors <- extract_authors(meta)
  
  expand_and_bind(file_path, authors)
}




# author helpers
# There are some potential node-names from which we could extract data on names.
# Here, we extract data from "string-name", "given-names", "surname", "prefix"
# and "suffix.
# node-names which are ignored are:
# "x", "aff", "collab", "xref", "bio", "address" and "degrees".
# We didn't investigate, what all of them
# might hold. "xref" seems to be used for footnotes, and "collab", when there
# are no authors but an institution. But this knowledge was not systematically
# corroborated.
extract_authors <- function(article) {
  # case for no authors
  if (is_empty(xml_find_all(article, ".//given-names")) &
      is_empty(xml_find_all(article, ".//surname")) &
      is_empty(xml_find_all(article, ".//string-name"))) {

    tibble::new_tibble(list(
      prefix = NA_character_,
      given_name = NA_character_,
      surname = NA_character_,
      string_name = NA_character_,
      suffix = NA_character_,
      author_number = NA_real_
    ))

    # case for authors with just "string-name"
  } else if (is_empty(xml_find_all(article, ".//given-names")) &
             is_empty(xml_find_all(article, ".//surname"))) {

    string_name <- xml_find_all(article, ".//string-name") %>% xml_text()

    missings <- rep(NA_character_, length(string_name))
    
    tibble::new_tibble(list(
      prefix = missings,
      given_name = missings,
      surname = missings,
      string_name = string_name,
      suffix = missings,
      author_number = seq_along(string_name)
    ))

    # standard case
  } else {
    contribs <- xml_find_all(article, ".//contrib")

    given_names <- extract_author_parts(contribs, ".//given-names")
    surnames <- extract_author_parts(contribs, ".//surname")
    prefix <- extract_author_parts(contribs, ".//prefix")
    suffix <- extract_author_parts(contribs, ".//suffix")

    # replace some trailing ","s after surnames (occurs when there is a suffix)
    surnames <- gsub(pattern = ",$", replacement =  "", x = surnames)

    tibble::new_tibble(list(
      prefix = prefix,
      given_name = given_names,
      surname = surnames,
      string_name = rep(NA_character_, length(given_names)),
      suffix = suffix,
      author_number = seq_along(given_names)
    ))
  }
}


extract_author_parts <- function(contribs, xpath) {
  contribs %>%
    map(xml_find_first, xpath) %>%
    map(xml_text) %>%
    map_if(is_empty, ~NA_character_) %>%
    flatten_chr()
}
