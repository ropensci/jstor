#' Extract the basename of a path
#' 
#' This helper simply extracts the basename of a path and removes the extension,
#' e.g. `foo/bar.txt` is shortened to `bar`.
#' 
#' @param file_path A path to a file
#' @export
get_basename <- function(file_path) {
  if (inherits(file_path, "jstor_zip")) {
    return(file_path[["file_path"]])
  } else {
    basename(file_path) %>%
      tools::file_path_sans_ext()
  }
}

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


expand_and_bind <- function(file_path, individual_part) {
  list(
    basename_id = get_basename(file_path) %>%
      rep(times = NROW(individual_part))
  ) %>%
    dplyr::bind_cols(individual_part)
}

#' simple helper to suppress warnings from invalid URIs. see issue #33
#' @noRd
read_jstor <- function(file) {
  
  if (identical(tools::file_ext(file), "xml")) {
    file <- check_path(file)
    
    suppressWarnings(xml2::read_xml(file))
  } else if (inherits(file, "jstor_zip")) {
    
    con <- unz(file$zip_archive, file$file_path, open = "rb")
    
    on.exit(close(con))
    
    xml2::read_xml(con)
    
  } else {
    stop("Unknown input file. Must be a `xml`-file.", 
         call. = FALSE)
  }
  
}

# copied from readr
check_path <- function(path) {
  if (file.exists(path))
    return(normalizePath(path, "/", mustWork = FALSE))
  
  stop("'", path, "' does not exist",
       if (!is_absolute_path(path))
         paste0(" in current working directory ('", getwd(), "')"),
       ".",
       call. = FALSE
  )
}

# copied from readr
is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}
