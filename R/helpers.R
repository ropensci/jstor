validate_file_path <- function(file_path, type) {
  if (identical(stringr::str_detect(file_path, type), FALSE)) {
    stop("`file_path` must be a `*.", type, "` file", call. = FALSE)
  }
}

validate_article <- function(xml_file) {
  if (identical(xml2::xml_name(xml_file), "book")) {
    stop(paste0("You are using `", sys.call(-1)[[1]], "` on a book. ", 
                "Please use `find_book` or `find_chapter` instead."),
                call. = FALSE)
  } else if (!identical(xml2::xml_name(xml_file), "article")) {
    stop("Unknown input file.")
  }
}

validate_book <- function(xml_file) {
  if (identical(xml2::xml_name(xml_file), "article")) {
    stop(paste0("You are using `", sys.call(-1)[[1]], "` on an article. ", 
                "Please use `find_article`, `find_authors`, `find_references` ",
                "or `find_footnotes` instead."),
         call. = FALSE)
  } else if (!identical(xml2::xml_name(xml_file), "book")) {
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

extract_first <- function(doc, element) {
  doc %>%
    xml_find_first(element) %>%
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

expand_and_bind <- function(file_path, individual_part) {
  list(
    basename_id = extract_basename(file_path, type = "xml") %>% 
      rep(times = nrow(individual_part))
  ) %>% 
    dplyr::bind_cols(individual_part)
}
