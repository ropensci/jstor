#' @importFrom purrr flatten_chr map map_chr map_if map_lgl is_empty discard
#' @importFrom dplyr mutate bind_rows as_data_frame n rename
#'                   mutate_all
#' @importFrom readr read_file write_csv guess_encoding locale cols read_csv
#'                   col_character col_integer
#' @importFrom xml2 xml_child xml_find_all xml_find_first xml_text xml_children
#' @importFrom stringr str_detect str_replace str_split str_replace_all
#' @importFrom tibble new_tibble
#' @importFrom rlang enquos get_expr eval_tidy abort
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @noRd
NULL


# global variables
utils::globalVariables(
  c("references", "footnotes", "encoding", "authors", "i", "Name", "Length",
    "Date", "chunk_number", ".", "fun_names", "meta_type", "type",
    "full_coverage", "title", "journal_id", "file_name", "id", "zip_loc",
    "first_page", "last_page", "splitted", "wo_space",  "wo_chars", "total",
    "page_range", "first", "last", "journal_doi", "journal_jcode",
    "journal_pub_id", "n_pages", ":=")
)
