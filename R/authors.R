#' Extract author information
#'
#' `find_authors()` extracts information about authors from JSTOR-XML files.
#'
#' **Authors**:
#'
#' A list-column with four possible columns:
#' - given_name
#' - surname
#' - author_number among co-authors
#' - string_name In some cases the name is not availible in separate fields, then
#' the full name is just available as a full string.
#'
#' With this approach some information is omitted: some authors may have a suffix
#' to their name, like _Jr_, or like _III_ (i.e. William the Third).
#' Those are currently not being extracted.
#'
#' @param file_path A file from JSTOR containing meta-data which can be read in with
#' [xml2::read_xml()]. `find_authors` takes care of importing the file.
#'
#' @return A `tibble` containing the extracted authors
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
