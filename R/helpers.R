validate_file_path <- function(file_path, type) {
  if (identical(stringr::str_detect(file_path, type), FALSE)) {
    stop("`file_path` must be a `*.", type, "` file", call. = FALSE)
  }
}

validate_doc_type <- function(xml_file, correct_type, wrong_type) {
  if (identical(xml2::xml_name(xml_file), wrong_type)) {
    stop(paste0("You are using `find_", correct_type, "` on an article. ", 
               "Please use `find_", wrong_type, "` instead."), call. = FALSE)
  } else if (!identical(xml2::xml_name(xml_file), correct_type)) {
    stop("Unknown input file.")
  }
}

# general helper to extract children from an XML-file
# should be be used with a string like "volume", or with an XPATH specification
# like ".//meta-value"
extract_child <- function(doc, element) {
  doc %>%
    xml_child(element) %>%
    xml_text()
}

extract_all <- function(doc, element) {
  res <- doc %>%
    xml_find_all(element)
  
  if (is_empty(res)) {
    return(NA_character_)
  } else {
    res %>%   
      xml_text() %>% 
      paste0(collapse = "; ")
  }
}

extract_basename <- function(file_path, type) {
  basename(file_path) %>%
    stringr::str_extract(paste0(".*?(?=\\.", type, ")"))
}


as_jstor <- function(x) {
  stopifnot(is.data.frame(x))
  
  structure(x, class = c("jstor", "data.frame"))
}
