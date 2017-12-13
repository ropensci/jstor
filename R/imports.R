#' @importFrom purrr flatten_chr map map_chr map_if map_lgl is_empty discard
#' @importFrom dplyr mutate bind_rows data_frame as_data_frame n rename mutate_all
#' @importFrom readr read_file write_csv guess_encoding locale
#' @importFrom xml2 xml_child xml_find_all xml_find_first xml_text xml_children
#' @importFrom stringr str_detect str_replace
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# global variables
utils::globalVariables(c("references", "footnotes", "encoding", "authors"))
