#' Extract all references
#'
#' This function extracts the content of `ref-list` within the `xml`-file. If
#' there are no references, a single `NA` is returend.
#'
#' @param file_path The path to the xml-file from which references should be
#'   extracted.
#'
#' @return A `tibble` with three columns containing the references:
#'
#' - `basename_id`: the identifier for the article the references come from
#' - `full_reference`: the text of the reference
#' - `author_names`: in case the information is present in the `xml`-file, names
#' of the authors for each article. If there is no information present, `NA`.
#'
#' Due to this structure, a single reference may span multiple rows. Each row
#' then has only one author in `author_names`. Mind the fact, that data quality
#' from JSTOR is mixed. For many articles there will be no data in `author_names`.
#' For some there will be data, but only incomplete, i.e. if the article has 3
#' authors, there might be only the first author in `author_names`.

#' @export
#' @examples 
#' find_references(jstor_example("sample_with_references.xml"))
find_references <- function(file_path) {
  validate_file_path(file_path, "xml")

  xml_file <- xml2::read_xml(file_path)

  out <- data.frame(basename_id = extract_basename(file_path, type = "xml"),
                    list(extract_references(xml_file)),
                    stringsAsFactors = FALSE)
  
  structure(out, class = c("jstor", "data.frame"))
}


extract_references <- function(xml_file) {
  res <- xml_find_all(xml_file, ".//ref-list")

  # if there are no references, exit and return NA
  if (is_empty(res)) {
    return(data.frame(full_reference = NA_character_,
                      author_names = NA_character_,
                      stringsAsFactors = FALSE)
           )
  }

  refs <- xml_children(res)

  full_string <- refs %>%
    map_chr(xml_text) %>%
    str_replace("^\\\n", "") # remove "\n" at beginning of strings

  full_string <- gsub("^$", NA_character_, full_string)
  
  author_strings <- refs %>%
    map(xml_find_all, ".//string-name") %>%
    map_if(is_empty, ~NA_character_) %>%
    map_if(any_not_missing, xml_text)


  frequencies <- lengths(author_strings)

  if (any(frequencies > 1)) {
    full_reference <- purrr::map2(full_string, frequencies, rep) %>%
      flatten_chr()
  } else {
    full_reference <- full_string
  }


  data.frame(full_reference  = full_reference,
             author_names = flatten_chr(author_strings),
             stringsAsFactors = FALSE)
}


any_not_missing <- function(x) any(!is.na(x))
