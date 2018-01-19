# find most likely encoding, but remove "windows-1252" before, since
# sometimes UTF-8 has a slightly lesser confidence, although it is the right
# encoding
get_encoding <- function(filename) {
  readr::guess_encoding(filename) %>%
    dplyr::filter(encoding != "windows-1252") %>%
    dplyr::select(encoding) %>%
    dplyr::slice(1L) %>%
    as.character()
}

#' Import full_text
#'
#' This function imports the full_text contents of a JSTOR-article.
#'
#' @param filename The path to the file.
#' @return A `data_frame`, containing the file-path as id, the full content of
#' the file, and the encoding which was used to read it in.
#'
#' @export
read_full_text <- function(filename) {
  validate_file_path(filename, "txt")

  id <- extract_basename(filename, type = "txt")

  encoding <- get_encoding(filename)

  text <- read_file(filename, locale = locale(encoding = encoding))

  out <- data.frame(basename_id = id, full_text = text, encoding = encoding,
                    stringsAsFactors = FALSE)
  
  structure(out, class = c("jstor", "data.frame"))
}
