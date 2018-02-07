#' Extract all footnotes
#'
#' This function extracts the content of `fn-group` from journal-articles.
#'
#' @param file_path The path to the `.xml`-file from which footnotes should be
#'   extracted.
#'
#' @return A `tibble` containing the content from `fn-group` (usually the
#' footnotes). If there were no footnotes, `NA_character` is returned for the
#' column `footnotes`.
#' 
#' @export
#' @examples 
#' find_footnotes(jstor_example("sample_with_footnotes.xml"))
find_footnotes <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  validate_article(xml_file)

  footnotes <- extract_footnotes(xml_file)

  expand_and_bind(file_path, footnotes)
}


extract_footnotes <- function(xml_file) {
  res <- xml_find_all(xml_file, ".//fn-group")

  # if there are no footnotes, exit and return NA
  if (is_empty(res)) {
    return(new_tibble(list(footnotes = NA_character_)))
  }

  res %>%
    xml_children() %>%
    map_chr(xml_text) %>%
    str_replace("^\\\n", "") %>%  # remove "\n" at beginning of strings
    list() %>%
    rlang::set_names("footnotes") %>%
    new_tibble()
}
