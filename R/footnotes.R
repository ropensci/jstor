#' Extract all footnotes
#'
#' This function extracts the content of `fn-group` from journal-articles.
#' 
#' The `fn-group` usually contains footnotes corresponding to the article.
#' However, since footnotes are currently not fully supported by DfR, 
#' there is no comprehensive documentation on the different variants. `jstor`
#' therefore extracts the content of `fn-group` exactly as it appears in the
#' data. Because of this, there might be other content present than footnotes.
#' 
#' In order to get all available information on citation data, you might need to
#' combine `jst_get_footnotes()` with `jst_get_references()`.
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
#' jst_get_footnotes(jst_example("article_with_footnotes.xml"))
jst_get_footnotes <- function(file_path) {
  xml_file <- read_jstor(file_path)

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
