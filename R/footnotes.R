#' Extract all footnotes
#'
#' This function extracts the content of `fn-group` within the `xml`-file. If
#' there are no footnotes, `NA` is returend.
#'
#' @param file_path The path to the xml-file from which footnotes should be
#'   extracted.
#'
#' @return A `tibble` containing the footnotes
#' @export
find_footnotes <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  out <- data.frame(basename_id = extract_basename(file_path, type = "xml"),
                    footnotes = extract_footnotes(xml_file),
                    stringsAsFactors = FALSE)
  out
}


extract_footnotes <- function(xml_file) {
  res <- xml_find_all(xml_file, ".//fn-group")

  # if there are no footnotes, exit and return NA
  if (is_empty(res)) {
    return(NA_character_)
  }

  res %>%
    xml_children() %>%
    map_chr(xml_text) %>%
    str_replace("^\\\n", "") # remove "\n" at beginning of strings
}
