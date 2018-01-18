#' Extract author information
#'
#' `find_authors()` extracts information about authors from JSTOR-XML files.
#'
#'
#' The function returns a `data.frame` with the following six columns:
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
#' @return A `data.frame` containing the extracted authors. All empty fields are
#' `NA_character`.
#'
#' @export
#' @examples 
#' find_authors(jstor_example("sample_with_references.xml"))
find_authors <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  front <- xml_find_all(xml_file, "front")
  article <- xml_child(front, "article-meta")

  out <- data.frame(
    basename_id = extract_basename(file_path, type = "xml"),
    extract_authors(article),
    stringsAsFactors = FALSE
  )

  structure(out, class = c("jstor", "data.frame"))
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

    data.frame(
      prefix = NA_character_,
      given_name = NA_character_,
      surname = NA_character_,
      string_name = NA_character_,
      suffix = NA_character_,
      author_number = NA_real_,
      stringsAsFactors = FALSE)

    # case for authors with just "string-name"
  } else if (is_empty(xml_find_all(article, ".//given-names")) &
             is_empty(xml_find_all(article, ".//surname"))) {

    string_name <- xml_find_all(article, ".//string-name") %>% xml_text()

    data.frame(
      prefix = NA_character_,
      given_name = NA_character_,
      surname = NA_character_,
      string_name = string_name,
      suffix = NA_character_,
      author_number = seq_along(string_name),
      stringsAsFactors = FALSE)

    # standard case
  } else {
    contribs <- xml_find_all(article, ".//contrib")

    given_names <- extract_author_parts(contribs, ".//given-names")
    surnames <- extract_author_parts(contribs, ".//surname")
    prefix <- extract_author_parts(contribs, ".//prefix")
    suffix <- extract_author_parts(contribs, ".//suffix")

    # replace some trailing ","s after surnames (occurs when there is a suffix)
    surnames <- gsub(pattern = ",$", replacement =  "", x = surnames)

    data.frame(
      prefix = prefix,
      given_name = given_names,
      surname = surnames,
      string_name = NA_character_,
      suffix = suffix,
      author_number = seq_along(given_names),
      stringsAsFactors = FALSE)
  }
}


extract_author_parts <- function(contribs, xpath) {
  contribs %>%
    map(xml_find_first, xpath) %>%
    map(xml_text) %>%
    map_if(is_empty, ~NA_character_) %>%
    flatten_chr()
}
